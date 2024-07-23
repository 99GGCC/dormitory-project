package com.dormitory.service.impl;

import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.BedStatusEnum;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.entity.BedInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.BedInfoMapper;
import com.dormitory.service.BedInfoService;
import com.dormitory.utils.CopyUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

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
public class BedInfoServiceImpl extends ServiceImpl<BedInfoMapper, BedInfo> implements BedInfoService {

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
}
