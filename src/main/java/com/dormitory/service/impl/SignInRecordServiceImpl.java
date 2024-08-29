package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.common.SignInRecordStatusEnum;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.qry.SignInRecordQry;
import com.dormitory.controller.vo.SignInRecordVO;
import com.dormitory.entity.SignInRecord;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SignInIssueMapper;
import com.dormitory.mapper.SignInRecordMapper;
import com.dormitory.service.SignInRecordService;
import com.dormitory.utils.RedisUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * <p>
 * 考勤记录表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class SignInRecordServiceImpl extends ServiceImpl<SignInRecordMapper, SignInRecord> implements SignInRecordService {

    /**
     * 签到信息Mapper
     */
    private final SignInIssueMapper signInIssueMapper;

    /**
     * Redis工具类
     */
    private final RedisUtil redisUtil;

    /**
     * 考勤记录分页查询
     *
     * @param qry 查询Qry
     * @return IPage<SignInRecordVO>
     */
    @Override
    public IPage<SignInRecordVO> pageByQry(SignInRecordQry qry) {
        return baseMapper.pageByQry(new Page<SignInRecordVO>(), qry);
    }

    /**
     * 给学生签到
     *
     * @param recordId 记录ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean sign(Long recordId) {
        // 根据记录ID查询记录信息
        SignInRecord record = baseMapper.selectById(recordId);
        // 判断记录是否为空
        if (ObjectUtils.isEmpty(record)) {
            throw new ServiceException("未查询到该学生的签到信息!");
        }
        // 判断签到是否过期
        if (redisUtil.hasKey(Constant.SIGN_IN_CACHE)) {
            // 更新签到学生人数
            signInIssueMapper.addNum(record.getSignInId());
            // 更新签到记录
            return new LambdaUpdateChainWrapper<>(baseMapper)
                    .eq(SignInRecord::getRecordId, recordId)
                    .set(SignInRecord::getRecordStatus, SignInRecordStatusEnum.SIGN.getCode())
                    .set(SignInRecord::getRecordTime, new Date())
                    .update();
        } else {
            throw new ServiceException("当前考勤已过期，无法继续签到!");
        }
    }

    /**
     * 学生签到
     *
     * @param recordId 记录ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean studentSign(Long recordId) {
        // 根据记录ID查询记录信息
        SignInRecord record = baseMapper.selectById(recordId);
        if (record == null) {
            throw new ServiceException("未查询到签到信息!");
        }
        if (!record.getStudentId().equals(StpStudentUtil.getLoginIdAsLong())) {
            throw new ServiceException("您不需要签到!");
        }
        // 调用考勤签到
        return sign(recordId);
    }

    /**
     * 考勤记录详情
     *
     * @param recordId 记录ID
     * @return SignInRecordVO
     */
    @Override
    public SignInRecordVO detailById(Long recordId) {
        return baseMapper.detailById(recordId);
    }
}
