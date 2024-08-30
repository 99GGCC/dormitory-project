package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.ApplyStatusEnum;
import com.dormitory.common.BooleanEnum;
import com.dormitory.common.RelocationTypeEnum;
import com.dormitory.common.UseStatusEnum;
import com.dormitory.config.StpStudentUtil;
import com.dormitory.controller.dto.ChangeApplyDTO;
import com.dormitory.controller.dto.ChangeApplyStatusDTO;
import com.dormitory.controller.qry.ChangeApplyQry;
import com.dormitory.controller.vo.ChangeApplyVO;
import com.dormitory.entity.*;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.*;
import com.dormitory.service.ChangeApplyService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 调换申请表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class ChangeApplyServiceImpl extends ServiceImpl<ChangeApplyMapper, ChangeApply> implements ChangeApplyService {

    /**
     * 动迁记录Mapper
     */
    private final RelocationRecordMapper relocationRecordMapper;

    /**
     * 学生信息Mapper
     */
    private final SysStudentMapper sysStudentMapper;

    /**
     * 床位信息Mapper
     */
    private final BedInfoMapper bedInfoMapper;

    /**
     * 宿舍信息Mapper
     */
    private final DormitoryInfoMapper dormitoryInfoMapper;

    /**
     * 调换申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ChangeApplyVO>
     */
    @Override
    public IPage<ChangeApplyVO> pageByQry(ChangeApplyQry qry) {
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }

    /**
     * 调换申请详情
     *
     * @param changeId 申请ID
     * @return ChangeApplyVO
     */
    @Override
    public ChangeApplyVO detailById(Long changeId) {
        return baseMapper.detailById(changeId);
    }

    /**
     * 发起调换申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean initiate(ChangeApplyDTO dto) {
        // 组装调换申请
        ChangeApply changeApply = CopyUtils.classCopy(dto, ChangeApply.class);
        // 补全申请时间、申请状态(管理员直接通过)
        changeApply.setApplyTime(new Date())
                .setApplyStatus(ApplyStatusEnum.PASS.getCode())
                .setApplyResult("管理员发起调换");
        // 处理学生宿舍信息
        handleStudentDormitoryInfo(changeApply);
        // 保存调换申请
        return baseMapper.insert(changeApply) > 0;
    }

    /**
     * 处理调换申请
     *
     * @param changeId 申请ID
     * @param dto      调换结果DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean status(Long changeId, ChangeApplyStatusDTO dto) {
        // 判断动迁状态
        ChangeApply changeApply = baseMapper.selectById(changeId);
        if (!ApplyStatusEnum.APPLY.getCode().equals(changeApply.getApplyStatus())) {
            throw new ServiceException("当前状态无法处理调换!");
        }
        // 判断是否是申请通过
        if (ApplyStatusEnum.PASS.getCode().equals(dto.getStatus())) {
            // 处理学生宿舍信息
            handleStudentDormitoryInfo(changeApply);
        }
        // 处理调换申请
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(ChangeApply::getChangeId, changeId)
                .set(ChangeApply::getApplyStatus, dto.getStatus())
                .set(ChangeApply::getApplyResult, dto.getApplyResult())
                .update();
    }

    /**
     * 学生调换申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ChangeApplyVO>
     */
    @Override
    public IPage<ChangeApplyVO> studentPageByQry(ChangeApplyQry qry) {
        // 设置学生ID
        qry.setStudentId(StpStudentUtil.getLoginIdAsLong());
        // 查询调换申请分页
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }

    /**
     * 学生发起调换申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean studentInitiate(ChangeApplyDTO dto) {
        // 获取登录学生ID
        long studentId = StpStudentUtil.getLoginIdAsLong();
        // 判断当前学生是否存在调换申请(状态为已申请)
        List<ChangeApply> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(ChangeApply::getStudentId, studentId)
                .eq(ChangeApply::getApplyStatus, ApplyStatusEnum.APPLY.getCode())
                .list();
        // 存在调换申请，提示无法重复发起
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("已存在调换申请，无法重复发起!");
        }
        // 组装调换申请
        ChangeApply changeApply = CopyUtils.classCopy(dto, ChangeApply.class);
        // 设置调换申请学生
        changeApply.setStudentId(studentId);
        // 补全申请时间、申请状态
        changeApply.setApplyTime(new Date())
                .setApplyStatus(ApplyStatusEnum.APPLY.getCode());
        // 保存调换申请
        return baseMapper.insert(changeApply) > 0;
    }

    /**
     * 学生取消调换申请
     *
     * @param changeId 申请ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean cancel(Long changeId) {
        // 更新调换申请状态和结果
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(ChangeApply::getChangeId, changeId)
                .set(ChangeApply::getApplyStatus, ApplyStatusEnum.CANCEL.getCode())
                .set(ChangeApply::getApplyResult, "学生主动取消")
                .update();
    }

    /**
     * 处理学生宿舍信息
     *
     * @param changeApply 调换申请实体
     */
    private void handleStudentDormitoryInfo(ChangeApply changeApply) {
        // 释放原床位和原宿舍
        BedInfo bedInfo = bedInfoMapper.selectById(changeApply.getBedId());
        // 判断是否是宿舍长
        if (BooleanEnum.TRUE.getCode().equals(bedInfo.getIsHead())) {
            // 是宿舍长，排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, changeApply.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .isNotNull(BedInfo::getUseStudent)
                    .list();
            // 如果宿舍里还有其他学生
            if (!CollectionUtils.isEmpty(list)) {
                // 选择第一个学生自动升级为宿舍长
                new LambdaUpdateChainWrapper<>(bedInfoMapper)
                        .eq(BedInfo::getBedId, list.get(0).getBedId())
                        .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                        .update();
            } else {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, changeApply.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        } else {
            // 排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getDormitoryId, bedInfo.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .isNotNull(BedInfo::getUseStudent)
                    .list();
            // 如果宿舍里没有学生
            if (CollectionUtils.isEmpty(list)) {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, bedInfo.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        }
        // 取消当前床位占用
        new LambdaUpdateChainWrapper<>(bedInfoMapper)
                .eq(BedInfo::getBedId, changeApply.getBedId())
                .set(BedInfo::getUseStudent, null)
                .update();
        // 占用新床位和新宿舍
        DormitoryInfo dormitoryInfo = dormitoryInfoMapper.selectById(changeApply.getInDormitoryId());
        // 判断宿舍是否使用
        if (UseStatusEnum.USE.getCode().equals(dormitoryInfo.getUseStatus())) {
            // 宿舍已使用，默认已存在宿舍长,只更新床位使用
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, changeApply.getInBedId())
                    .set(BedInfo::getUseStudent, changeApply.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.FALSE.getCode())
                    .update();
        } else {
            // 宿舍未使用，当前学生自动升级为宿舍长
            new LambdaUpdateChainWrapper<>(bedInfoMapper)
                    .eq(BedInfo::getBedId, changeApply.getInBedId())
                    .set(BedInfo::getUseStudent, changeApply.getStudentId())
                    .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                    .update();
            // 更新宿舍状态为已使用
            new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                    .eq(DormitoryInfo::getDormitoryId, dormitoryInfo.getDormitoryId())
                    .set(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                    .update();
        }
        // 更新学生信息中的床位信息
        new LambdaUpdateChainWrapper<>(sysStudentMapper)
                .eq(SysStudent::getStudentId, changeApply.getStudentId())
                .set(SysStudent::getDormitoryId, changeApply.getInDormitoryId())
                .set(SysStudent::getBedId, changeApply.getInBedId())
                .update();
        // 记录动迁记录
        recordRelocationRecord(changeApply);
    }

    /**
     * 记录动迁记录（迁入迁出）
     *
     * @param changeApply 调换申请实体
     */
    private void recordRelocationRecord(ChangeApply changeApply) {
        // 记录动迁记录(调换迁出)
        RelocationRecord outRelocationRecord = new RelocationRecord()
                .setStudentId(changeApply.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(changeApply.getDormitoryId())
                .setBedId(changeApply.getBedId())
                .setRelocationType(RelocationTypeEnum.CHANGE_OUT.getCode());
        // 保存调换迁出
        relocationRecordMapper.insert(outRelocationRecord);
        // 记录动迁记录(调换迁入)
        RelocationRecord inRelocationRecord = new RelocationRecord()
                .setStudentId(changeApply.getStudentId())
                .setRelocationTime(new Date())
                .setDormitoryId(changeApply.getInDormitoryId())
                .setBedId(changeApply.getInBedId())
                .setRelocationType(RelocationTypeEnum.CHANGE_IN.getCode());
        // 保存调换迁入
        relocationRecordMapper.insert(inRelocationRecord);
    }
}
