package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.DormitoryStatusEnum;
import com.dormitory.common.UseStatusEnum;
import com.dormitory.controller.dto.BuildingInfoDTO;
import com.dormitory.controller.qry.BuildingInfoQry;
import com.dormitory.controller.vo.BuildingInfoVO;
import com.dormitory.entity.BuildingInfo;
import com.dormitory.entity.DormitoryInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.BuildingInfoMapper;
import com.dormitory.service.BuildingInfoService;
import com.dormitory.service.DormitoryInfoService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.IdUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 楼栋信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class BuildingInfoServiceImpl extends ServiceImpl<BuildingInfoMapper, BuildingInfo> implements BuildingInfoService {

    /**
     * 宿舍信息Service
     */
    private final DormitoryInfoService dormitoryInfoService;

    /**
     * 楼栋信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<BuildingInfoVO>
     */
    @Override
    @Transactional
    public IPage<BuildingInfoVO> pageByQry(BuildingInfoQry qry) {
        IPage<BuildingInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                // building_name like ?
                // building_name = ?
                .like(StringUtils.isNotEmpty(qry.getBuildingName()), BuildingInfo::getBuildingName, qry.getBuildingName())
                .eq(ObjectUtils.isNotEmpty(qry.getBuildingType()), BuildingInfo::getBuildingType, qry.getBuildingType())
                .like(StringUtils.isNotEmpty(qry.getBuildingAdmin()), BuildingInfo::getBuildingAdmin, qry.getBuildingAdmin())
                .page(pages);
        IPage<BuildingInfoVO> page = CopyUtils.covertPage(pages, BuildingInfoVO.class);
        List<BuildingInfoVO> buildingInfoVOList = page.getRecords();

        for (BuildingInfoVO buildingInfoVO : buildingInfoVOList) {
            // 总房间数量
            buildingInfoVO.setAllRoomNum(buildingInfoVO.getRoomNum() * buildingInfoVO.getBuildingFloor());
            LambdaQueryWrapper<DormitoryInfo> useWrapper = new LambdaQueryWrapper<>();
            // 使用的房间数量
            useWrapper.eq(DormitoryInfo::getBuildingId, buildingInfoVO.getBuildingId())
                    .eq(DormitoryInfo::getUseStatus, 1);
            int useCount = dormitoryInfoService.count(useWrapper);
            buildingInfoVO.setUseRoomNum(useCount);
            // 禁用的房间数量
            LambdaQueryWrapper<DormitoryInfo> disableWrapper = new LambdaQueryWrapper<>();
            disableWrapper.eq(DormitoryInfo::getBuildingId, buildingInfoVO.getBuildingId())
                    .eq(DormitoryInfo::getDormitoryStatus, 0);
            int disableCount = dormitoryInfoService.count(disableWrapper);
            buildingInfoVO.setDisableRoomNum(disableCount);
            // 空闲房间数量
            int idleCount = buildingInfoVO.getRoomNum() * buildingInfoVO.getBuildingFloor() - disableCount - useCount;
            buildingInfoVO.setIdleRoomNum(idleCount);
        }
        page.setRecords(buildingInfoVOList);
        return page;
    }

    /**
     * 楼栋信息列表查询
     *
     * @param qry 查询Qry
     * @return List<BuildingInfoVO>
     */
    @Override
    public List<BuildingInfoVO> listByQry(BuildingInfoQry qry) {
        List<BuildingInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getBuildingName()), BuildingInfo::getBuildingName, qry.getBuildingName())
                .eq(ObjectUtils.isNotEmpty(qry.getBuildingType()), BuildingInfo::getBuildingType, qry.getBuildingType())
                .like(StringUtils.isNotEmpty(qry.getBuildingAdmin()), BuildingInfo::getBuildingAdmin, qry.getBuildingAdmin())
                .list();
        return CopyUtils.classCopyList(list, BuildingInfoVO.class);
    }

    /**
     * 楼栋信息详情
     *
     * @param buildingId 楼栋ID
     * @return BuildingInfoVO
     */
    @Override
    public BuildingInfoVO detailById(Long buildingId) {
        return baseMapper.detailById(buildingId);
    }

    /**
     * 新增楼栋信息
     *
     * @param dto 楼栋DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(BuildingInfoDTO dto) {
        // 判断楼栋信息是否存在
        List<BuildingInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(BuildingInfo::getBuildingName, dto.getBuildingName())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("楼栋信息已存在,新增失败!");
        }
        // 复制楼栋信息实体
        BuildingInfo buildingInfo = CopyUtils.classCopy(dto, BuildingInfo.class);
        // 预生成楼栋ID
        buildingInfo.setBuildingId(IdUtils.getLongId());
        // 生成楼栋房间信息
        List<DormitoryInfo> dormitoryInfoList = generateDormitoryInfos(buildingInfo);
        // 批量保存宿舍信息
        dormitoryInfoService.saveBatch(dormitoryInfoList);
        // 保存楼栋信息，并返回结果
        return baseMapper.insert(buildingInfo) > 0;
    }

    /**
     * 编辑楼栋信息
     *
     * @param buildingId 楼栋ID
     * @param dto        楼栋DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long buildingId, BuildingInfoDTO dto) {
        // 判断楼栋信息是否存在
        List<BuildingInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(BuildingInfo::getBuildingName, dto.getBuildingName())
                .ne(BuildingInfo::getBuildingId, buildingId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("楼栋信息已存在,编辑失败!");
        }
        // 查询数据库楼栋信息
        BuildingInfo buildingInfo = baseMapper.selectById(buildingId);
        // 判断楼栋房间和楼层是否变动
        if (!buildingInfo.getBuildingFloor().equals(dto.getBuildingFloor()) || !buildingInfo.getRoomNum().equals(dto.getRoomNum())) {
            // 判断楼栋房间是否被使用
            // 只要存在有使用的房间 宿舍的楼层和房间数就不能改变了
            List<DormitoryInfo> useList = new LambdaQueryChainWrapper<>(dormitoryInfoService.getBaseMapper())
                    .eq(DormitoryInfo::getBuildingId, buildingId)
                    .eq(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                    .list();
            if (!CollectionUtils.isEmpty(useList)) {
                throw new ServiceException("楼栋房间已被使用，无法编辑!");
            }
            // 复制楼栋信息实体
            buildingInfo = CopyUtils.classCopy(dto, BuildingInfo.class);
            // 设置楼栋ID
            buildingInfo.setBuildingId(buildingId);
            // 删除楼栋房间信息
            new LambdaUpdateChainWrapper<>(dormitoryInfoService.getBaseMapper())
                    .eq(DormitoryInfo::getBuildingId, buildingId)
                    .remove();
            // 生成楼栋房间信息w
            List<DormitoryInfo> dormitoryInfoList = this.generateDormitoryInfos(buildingInfo);

            // 批量保存宿舍信息
            dormitoryInfoService.saveBatch(dormitoryInfoList);
        }
        // 复制楼栋信息实体
        buildingInfo = CopyUtils.classCopy(dto, BuildingInfo.class);
        // 设置楼栋ID
        buildingInfo.setBuildingId(buildingId);
        // 编辑楼栋信息，并返回结果
        return baseMapper.updateById(buildingInfo) > 0;
    }

    /**
     * 删除楼栋信息
     *
     * @param buildingId 楼栋ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long buildingId) {
        // 判断楼栋房间是否被使用
        List<DormitoryInfo> useList = new LambdaQueryChainWrapper<>(dormitoryInfoService.getBaseMapper())
                .eq(DormitoryInfo::getBuildingId, buildingId)
                .eq(DormitoryInfo::getUseStatus, UseStatusEnum.USE.getCode())
                .list();
        if (!CollectionUtils.isEmpty(useList)) {
            throw new ServiceException("楼栋房间已被使用，无法删除楼栋!");
        }
        // 根据ID删除楼栋信息
        return baseMapper.deleteById(buildingId) > 0;
    }


    /**
     * 生成楼栋房间信息
     *
     * @param buildingInfo 楼栋信息
     * @return List<DormitoryInfo>
     */
    private List<DormitoryInfo> generateDormitoryInfos(BuildingInfo buildingInfo) {
        // 生成楼栋房间信息
        List<DormitoryInfo> dormitoryInfoList = new ArrayList<>();
        for (int i = 0; i < buildingInfo.getBuildingFloor(); i++) {
            for (int j = 0; j < buildingInfo.getRoomNum(); j++) {
                dormitoryInfoList.add(
                        new DormitoryInfo()
                                .setBuildingId(buildingInfo.getBuildingId())
                                .setDormitoryName(buildingInfo.getShortName() + (i + 1) + ((j + 1) >= 10 ? (j + 1) : "0" + (j + 1)))
                                .setBuildingFloor(i + 1)
                                .setDormitoryStatus(DormitoryStatusEnum.ENABLE.getCode())
                                .setUseStatus(UseStatusEnum.NOT_USE.getCode())
                );
            }
        }
        return dormitoryInfoList;
    }
}
