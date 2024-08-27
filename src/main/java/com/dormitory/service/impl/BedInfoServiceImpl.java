package com.dormitory.service.impl;

import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.*;
import com.dormitory.controller.dto.ArrangeBedDTO;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.controller.dto.ReleaseBedDTO;
import com.dormitory.controller.qry.BedInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.entity.*;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.*;
import com.dormitory.service.BedInfoService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;

/**
 * <p>
 * 床位信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class BedInfoServiceImpl extends ServiceImpl<BedInfoMapper, BedInfo> implements BedInfoService {

    /**
     * 学生信息Mapper
     */
    private final SysStudentMapper sysStudentMapper;

    /**
     * 班级信息mapper
     */
    private final ClassesInfoMapper classesInfoMapper;

    /**
     * 动迁记录Mapper
     */
    private final RelocationRecordMapper relocationRecordMapper;

    /**
     * 宿舍信息Mapper
     */
    private final DormitoryInfoMapper dormitoryInfoMapper;

    /**
     * 床位信息列表查询
     *
     * @param qry 查询Qry
     * @return List<BedInfoVO>·
     */
    @Override
    public List<BedInfoVO> listByQry(BedInfoQry qry) {
        return baseMapper.listByQry(qry);
    }

    /**
     * 新增床位信息
     *
     * @param dto 床位DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(BedInfoDTO dto) {
        // 判断床位是否存在
        List<BedInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(BedInfo::getDormitoryId, dto.getDormitoryId())
                .eq(BedInfo::getBedName, dto.getBedName())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("床位信息已存在,新增失败!");
        }
        // 复制床位信息实体
        BedInfo bedInfo = CopyUtils.classCopy(dto, BedInfo.class);
        // 设置宿舍长为否
        bedInfo.setIsHead(BooleanEnum.FALSE.getCode());
        // 保存床位信息，并返回结果
        return baseMapper.insert(bedInfo) > 0;
    }

    /**
     * 编辑床位信息
     *
     * @param bedId 床位ID
     * @param dto   床位DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long bedId, BedInfoDTO dto) {
        // 判断床位是否存在
        List<BedInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(BedInfo::getDormitoryId, dto.getDormitoryId())
                .eq(BedInfo::getBedName, dto.getBedName())
                .ne(BedInfo::getBedId, bedId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("床位信息已存在,修改失败!");
        }
        BedInfo bedInfo = baseMapper.selectById(bedId);
        // 判断是否是禁用床位
        if (BedStatusEnum.DISABLE.getCode().equals(dto.getBedStatus())) {
            if (!ObjectUtils.isEmpty(bedInfo.getUseStudent())) {
                throw new ServiceException("当前床位已被学生使用,无法禁用!");
            }
        }
        // 复制床位信息实体
        bedInfo = CopyUtils.classCopy(dto, BedInfo.class);
        // 设置床位ID
        bedInfo.setBedId(bedId);
        // 设置宿舍长为否
        bedInfo.setIsHead(BooleanEnum.FALSE.getCode());
        // 修改床位信息，并返回结果
        return baseMapper.updateById(bedInfo) > 0;
    }

    /**
     * 删除床位信息
     *
     * @param bedId 床位ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long bedId) {
        BedInfo bedInfo = baseMapper.selectById(bedId);
        // 判断是否被学生使用
        if (!ObjectUtils.isEmpty(bedInfo.getUseStudent())) {
            throw new ServiceException("当前床位已被学生使用,无法删除!");
        }
        // 删除床位信息
        return baseMapper.deleteById(bedId) > 0;
    }

    /**
     * 安排床位
     *
     * @param dto 安排DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean arrange(ArrangeBedDTO dto) {
        // 根据床位ID查询床位信息
        BedInfo bedInfo = baseMapper.selectById(dto.getBedId());
        // 判断床位是否禁用
        if (BedStatusEnum.DISABLE.getCode().equals(bedInfo.getBedStatus())) {
            throw new ServiceException("床位禁用，无法安排学生!");
        }
        // 判断床位是否被使用
        if (!ObjectUtils.isEmpty(bedInfo.getUseStudent())) {
            throw new ServiceException("床位已被使用!");
        }
        // 判断学生是否存在
        SysStudent student = sysStudentMapper.selectById(dto.getUseStudent());
        if (ObjectUtils.isEmpty(student)) {
            throw new ServiceException("未查询到学生信息!");
        }
        // 判断床位是否已被使用
        List<BedInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(BedInfo::getUseStudent, dto.getUseStudent())
                .list();
        // 判断学生是否已有床位
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("该学生已安排床位，无法再次安排!");
        }
        // 判断该宿舍是否已存在宿舍长
        if (BooleanEnum.TRUE.getCode().equals(dto.getIsHead())) {
            List<BedInfo> list1 = new LambdaQueryChainWrapper<>(baseMapper)
                    .eq(BedInfo::getDormitoryId, dto.getDormitoryId())
                    .eq(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                    .list();
            if (!CollectionUtils.isEmpty(list1)) {
                throw new ServiceException("该宿舍已有宿舍长，无法再次设置!");
            }
        }
        // 判断该学生是否能够安排入住（是否已毕业）
        ClassesInfo classesInfo = classesInfoMapper.selectById(student.getClassesId());
        if (ObjectUtils.isEmpty(classesInfo) || ClassesStatusEnum.GRADUATED.getCode().equals(classesInfo.getClassesStatus())) {
            throw new ServiceException("该班级已毕业，无法继续安排学生入住!");
        }
        // 设置学生床位
        new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(BedInfo::getBedId, dto.getBedId())
                .set(BedInfo::getUseStudent, dto.getUseStudent())
                .set(BedInfo::getIsHead, dto.getIsHead())
                .update();
        // 学生表设置学生床位ID
        new LambdaUpdateChainWrapper<>(sysStudentMapper)
                .eq(SysStudent::getStudentId, dto.getUseStudent())
                .set(SysStudent::getBedId, dto.getBedId())
                .set(SysStudent::getDormitoryId, dto.getDormitoryId())
                .update();
        DormitoryInfo dormitoryInfo = new LambdaQueryChainWrapper<>(dormitoryInfoMapper)
                .eq(DormitoryInfo::getDormitoryId, dto.getDormitoryId())
                .one();
        // 判断宿舍是否使用
        if (UseStatusEnum.NOT_USE.getCode().equals(dormitoryInfo.getUseStatus())) {
            // 未使用，设置宿舍使用状态
            new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                    .eq(DormitoryInfo::getDormitoryId, dto.getDormitoryId())
                    .set(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                    .update();
        }
        // 记录动迁信息
        return relocationRecordMapper.insert(
                new RelocationRecord()
                        .setBedId(dto.getBedId())
                        .setDormitoryId(dto.getDormitoryId())
                        .setRelocationTime(new Date())
                        .setRelocationType(RelocationTypeEnum.FIRST_IN.getCode())
                        .setStudentId(dto.getUseStudent())
        ) > 0;
    }

    /**
     * 释放床位
     *
     * @param dto 释放DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean release(ReleaseBedDTO dto) {
        // 根据床位ID查询床位信息
        BedInfo bedInfo = baseMapper.selectById(dto.getBedId());
        // 判断是否是宿舍长
        if (BooleanEnum.TRUE.getCode().equals(bedInfo.getIsHead())) {
            // 是宿舍长，排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                    .eq(BedInfo::getDormitoryId, bedInfo.getDormitoryId())
                    .ne(BedInfo::getBedId, bedInfo.getBedId())
                    .isNotNull(BedInfo::getUseStudent)
                    .list();
            // 如果宿舍里还有其他学生
            if (!CollectionUtils.isEmpty(list)) {
                // 选择第一个学生自动升级为宿舍长
                new LambdaUpdateChainWrapper<>(baseMapper)
                        .eq(BedInfo::getBedId, list.get(0).getBedId())
                        .set(BedInfo::getIsHead, BooleanEnum.TRUE.getCode())
                        .update();
            } else {
                // 当前宿舍不存在学生，释放宿舍
                new LambdaUpdateChainWrapper<>(dormitoryInfoMapper)
                        .eq(DormitoryInfo::getDormitoryId, bedInfo.getDormitoryId())
                        .set(DormitoryInfo::getUseStatus, UseStatusEnum.NOT_USE.getCode())
                        .update();
            }
        } else {
            // 排除当前学生查询其他学生
            List<BedInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
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
        // 释放床位信息
        new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(BedInfo::getBedId, dto.getBedId())
                .set(BedInfo::getUseStudent, null)
                .set(BedInfo::getIsHead, BooleanEnum.FALSE.getCode())
                .update();
        // 学生表释放
        new LambdaUpdateChainWrapper<>(sysStudentMapper)
                .eq(SysStudent::getStudentId, bedInfo.getUseStudent())
                .set(SysStudent::getBedId, null)
                .set(SysStudent::getDormitoryId, null)
                .update();
        // 记录动迁信息
        return relocationRecordMapper.insert(
                new RelocationRecord()
                        .setBedId(dto.getBedId())
                        .setDormitoryId(bedInfo.getDormitoryId())
                        .setRelocationTime(new Date())
                        .setRelocationType(dto.getRelocationType())
                        .setStudentId(bedInfo.getUseStudent())
        ) > 0;
    }
}
