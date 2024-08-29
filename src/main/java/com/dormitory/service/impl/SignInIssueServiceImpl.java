package com.dormitory.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.BooleanEnum;
import com.dormitory.common.Constant;
import com.dormitory.common.SignInRecordStatusEnum;
import com.dormitory.controller.dto.SignInIssueDTO;
import com.dormitory.controller.qry.SignInIssueQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.controller.vo.SignInBuildingVO;
import com.dormitory.controller.vo.SignInIssueVO;
import com.dormitory.entity.SignInBuilding;
import com.dormitory.entity.SignInIssue;
import com.dormitory.entity.SignInRecord;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.BedInfoMapper;
import com.dormitory.mapper.SignInBuildingMapper;
import com.dormitory.mapper.SignInIssueMapper;
import com.dormitory.mapper.SignInRecordMapper;
import com.dormitory.service.AsyncService;
import com.dormitory.service.SignInIssueService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.DateUtils;
import com.dormitory.utils.IdUtils;
import com.dormitory.utils.RedisUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 考勤发布表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class SignInIssueServiceImpl extends ServiceImpl<SignInIssueMapper, SignInIssue> implements SignInIssueService {

    /**
     * 考勤宿舍Mapper
     */
    private final SignInBuildingMapper signInBuildingMapper;

    /**
     * 考勤信息Mapper
     */
    private final SignInRecordMapper signInRecordMapper;

    /**
     * 床位信息Mapper
     */
    private final BedInfoMapper bedInfoMapper;

    /**
     * 异步执行Service
     */
    private final AsyncService asyncService;

    /**
     * redis工具类
     */
    private final RedisUtil redisUtil;

    /**
     * 考勤信息分页查询
     *
     * @param qry qry
     * @return IPage<SignInIssueVO>
     */
    @Override
    public IPage<SignInIssueVO> pageByQry(SignInIssueQry qry) {
        // 根据楼栋ID查询楼栋信息VO
        List<SignInBuildingVO> signInBuildingVOList = new ArrayList<>();
        // 判断查询楼栋ID是否为空
        if (!CollectionUtils.isEmpty(qry.getBuildingIds())) {
            signInBuildingVOList = signInBuildingMapper.listByBuildingIds(qry.getBuildingIds());
        }
        IPage<SignInIssue> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .between(ObjectUtils.isNotEmpty(qry.getEndTimeStart()) && ObjectUtils.isNotEmpty(qry.getEndTimeEnd()),
                        SignInIssue::getEndTime, qry.getEndTimeStart(), qry.getEndTimeEnd())
                .in(!CollectionUtils.isEmpty(signInBuildingVOList), SignInIssue::getSignInId, signInBuildingVOList.stream().map(SignInBuildingVO::getSignInId).distinct().collect(Collectors.toList()))
                .page(pages);
        // 转换成VO分页对象
        IPage<SignInIssueVO> signInIssueVOPage = CopyUtils.covertPage(pages, SignInIssueVO.class);
        // 判断数据是否存在
        if (!CollectionUtils.isEmpty(signInIssueVOPage.getRecords())) {
            signInBuildingVOList = signInBuildingMapper.listBySignInIds(signInIssueVOPage.getRecords().stream().map(SignInIssueVO::getSignInId).collect(Collectors.toList()));
            if (!CollectionUtils.isEmpty(signInBuildingVOList)) {
                // 通过考勤ID对考勤楼栋进行分组
                Map<Long, List<SignInBuildingVO>> collect = signInBuildingVOList.stream().collect(Collectors.groupingBy(SignInBuildingVO::getSignInId));
                // 循环设置考勤楼栋数据
                for (SignInIssueVO record : signInIssueVOPage.getRecords()) {
                    record.setBuildingVOList(collect.get(record.getSignInId()));
                }
            }
        }
        // 返回签到分页列表
        return signInIssueVOPage;
    }

    /**
     * 考勤信息详情
     *
     * @param signInId 考勤ID
     * @return SignInIssueVO
     */
    @Override
    public SignInIssueVO detailById(Long signInId) {
        // 查询考勤信息
        SignInIssueVO signInIssueVO = CopyUtils.classCopy(baseMapper.selectById(signInId), SignInIssueVO.class);
        if (ObjectUtils.isNotEmpty(signInIssueVO)) {
            // 根据考勤ID查询考勤楼栋
            signInIssueVO.setBuildingVOList(signInBuildingMapper.listBySignInIds(Collections.singletonList(signInId)));
        }
        // 返回考勤详情
        return signInIssueVO;
    }

    /**
     * 考勤信息发布
     *
     * @param dto 发布DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(SignInIssueDTO dto) {
        // 获取楼栋下所有宿舍的入住人员
        List<BedInfoVO> bedInfos = bedInfoMapper.selectByBuildingIds(dto.getBuildingIds());
        // 判断是否存在入住人员
        if (CollectionUtils.isEmpty(bedInfos)) {
            throw new ServiceException("当前宿舍不存在入住学生，无法发起考勤!");
        }
        long times = DateUtils.calculateSecondsUntil(dto.getEndTime());
        if (times <= 0) {
            throw new ServiceException("结束时间不能小于等于当前时间!");
        }
        // 组装考勤信息
        SignInIssue signInIssue = CopyUtils.classCopy(dto, SignInIssue.class);
        signInIssue.setAdminId(StpUtil.getLoginIdAsLong())
                .setIssueTime(new Date())
                .setTotalStudent(bedInfos.size())
                .setRealityStudent(Constant.INTEGER_ZERO)
                .setSignInStatus(BooleanEnum.TRUE.getCode())
                .setSignInId(IdUtils.getLongId());
        // 组装考勤楼栋列表
        dto.getBuildingIds().forEach(id -> {
            // 保存考勤楼栋
            signInBuildingMapper.insert(new SignInBuilding().setBuildingId(id).setSignInId(signInIssue.getSignInId()));
        });
        // 保存考勤信息
        baseMapper.insert(signInIssue);
        // 考勤信息缓存redis(保存时限为结束时间之前)
        redisUtil.set(Constant.SIGN_IN_CACHE + signInIssue.getSignInId(), signInIssue.getSignInId(), times);
        // 异步发送考勤邮件
        asyncService.sendEmail(signInIssue, bedInfos);
        // 返回成功
        return true;
    }

    /**
     * 考勤信息作废
     *
     * @param signInId 考勤ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean status(Long signInId) {
        // 更新考勤记录状态为取消
        new LambdaUpdateChainWrapper<>(signInRecordMapper)
                .eq(SignInRecord::getSignInId, signInId)
                .set(SignInRecord::getRecordStatus, SignInRecordStatusEnum.CANCEL.getCode())
                .update();
        // 更新考勤信息状态
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(SignInIssue::getSignInId, signInId)
                .set(SignInIssue::getSignInStatus, BooleanEnum.FALSE.getCode())
                .update();
    }
}
