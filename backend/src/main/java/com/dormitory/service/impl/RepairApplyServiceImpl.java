package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.RepairStatusEnum;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.dto.RepairApplyDTO;
import com.dormitory.controller.qry.RepairApplyQry;
import com.dormitory.controller.vo.RepairApplyVO;
import com.dormitory.entity.RepairApply;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.RepairApplyMapper;
import com.dormitory.service.RepairApplyService;
import com.dormitory.utils.CopyUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;

/**
 * <p>
 * 维修申请表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
public class RepairApplyServiceImpl extends ServiceImpl<RepairApplyMapper, RepairApply> implements RepairApplyService {

    /**
     * 维修申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RepairApplyVO>
     */
    @Override
    public IPage<RepairApplyVO> pageByQry(RepairApplyQry qry) {
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }

    /**
     * 维修申请详情
     *
     * @param repairId 申请ID
     * @return RepairApplyVO
     */
    @Override
    public RepairApplyVO detailById(Long repairId) {
        return baseMapper.detailById(repairId);
    }

    /**
     * 处理维修申请
     *
     * @param repairId 申请ID
     * @param status   维修状态
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean status(Long repairId, Integer status) {
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(RepairApply::getRepairId, repairId)
                .set(RepairApply::getRepairStatus, status)
                .set(RepairApply::getRepairResult, RepairStatusEnum.HANDLE.getCode().equals(status) ? RepairStatusEnum.HANDLE.getCode() : RepairStatusEnum.NO_HANDLE.getCode())
                .update();
    }

    /**
     * 学生维修申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RepairApplyVO>
     */
    @Override
    public IPage<RepairApplyVO> studentPageByQry(RepairApplyQry qry) {
        // 设置学生ID
        qry.setStudentId(StpStudentUtil.getLoginIdAsLong());
        // 分页查询
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }

    /**
     * 发起维修申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean initiate(RepairApplyDTO dto) {
        // 设置学生ID
        dto.setStudentId(StpStudentUtil.getLoginIdAsLong());
        // 组装维修申请
        RepairApply repairApply = CopyUtils.classCopy(dto, RepairApply.class);
        // 设置默认值
        repairApply.setApplyTime(new Date())
                .setRepairStatus(RepairStatusEnum.APPLY.getCode())
                .setRepairResult(RepairStatusEnum.APPLY.getCode());
        // 保存维修申请
        return baseMapper.insert(repairApply) > 0;
    }

    /**
     * 取消维修申请
     *
     * @param repairId 申请ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean cancel(Long repairId) {
        // 校验维修申请
        RepairApply repairApply = baseMapper.selectById(repairId);
        long studentId = StpStudentUtil.getLoginIdAsLong();
        // 判断维修申请是否是本人
        if (!repairApply.getStudentId().equals(studentId)) {
            throw new ServiceException("不能取消别人的维修申请!");
        }
        // 取消维修申请
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(RepairApply::getRepairId, repairId)
                .set(RepairApply::getRepairStatus, RepairStatusEnum.CANCEL.getCode())
                .update();
    }
}
