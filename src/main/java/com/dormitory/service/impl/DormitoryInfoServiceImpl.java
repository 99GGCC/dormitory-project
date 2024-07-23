package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.BedStatusEnum;
import com.dormitory.common.DormitoryStatusEnum;
import com.dormitory.controller.dto.SetBedDTO;
import com.dormitory.controller.qry.DormitoryInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.controller.vo.DormitoryInfoVO;
import com.dormitory.entity.BedInfo;
import com.dormitory.entity.DormitoryInfo;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.DormitoryInfoMapper;
import com.dormitory.service.BedInfoService;
import com.dormitory.service.DormitoryInfoService;
import com.dormitory.utils.CopyUtils;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * 宿舍信息表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class DormitoryInfoServiceImpl extends ServiceImpl<DormitoryInfoMapper, DormitoryInfo> implements DormitoryInfoService {

    /**
     * 床位信息Service
     */
    private final BedInfoService bedInfoService;

    /**
     * 宿舍信息分页查询
     *
     * @param qry 查询Qry
     * @return IPage<DormitoryInfoVO>
     */
    @Override
    public IPage<DormitoryInfoVO> pageByQry(DormitoryInfoQry qry) {
        IPage<DormitoryInfo> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getDormitoryName()), DormitoryInfo::getDormitoryName, qry.getDormitoryName())
                .eq(ObjectUtils.isNotEmpty(qry.getBuildingId()), DormitoryInfo::getBuildingId, qry.getBuildingId())
                .eq(ObjectUtils.isNotEmpty(qry.getBuildingFloor()), DormitoryInfo::getBuildingFloor, qry.getBuildingFloor())
                .eq(ObjectUtils.isNotEmpty(qry.getDormitoryStatus()), DormitoryInfo::getDormitoryStatus, qry.getDormitoryStatus())
                .eq(ObjectUtils.isNotEmpty(qry.getUseStatus()), DormitoryInfo::getUseStatus, qry.getUseStatus())
                .page(pages);
        return CopyUtils.covertPage(pages, DormitoryInfoVO.class);
    }

    /**
     * 宿舍信息楼层列表查询
     *
     * @param buildingId 楼栋ID
     * @return Map<Integer, List < DormitoryInfoVO>>
     */
    @Override
    public Map<Integer, List<DormitoryInfoVO>> listByBuildingId(String buildingId) {
        List<DormitoryInfo> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(DormitoryInfo::getBuildingId, buildingId)
                .orderByAsc(DormitoryInfo::getBuildingFloor)
                .list();
        List<DormitoryInfoVO> voList = CopyUtils.classCopyList(list, DormitoryInfoVO.class);
        // 根据楼层信息分组返回
        return voList.stream().collect(Collectors.groupingBy(DormitoryInfoVO::getBuildingFloor));
    }

    /**
     * 宿舍信息详情
     *
     * @param dormitoryId 宿舍ID
     * @return DormitoryInfoVO
     */
    @Override
    public DormitoryInfoVO detail(Long dormitoryId) {
        return baseMapper.detail(dormitoryId);
    }

    /**
     * 设置宿舍状态
     *
     * @param dormitoryId 宿舍ID
     * @param status      状态
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean status(Long dormitoryId, Integer status) {
        // 查询宿舍详情
        DormitoryInfoVO detail = baseMapper.detail(dormitoryId);
        // 判断是否是禁用宿舍
        if (DormitoryStatusEnum.DISABLE.getCode().equals(status)) {
            for (BedInfoVO bedInfoVO : detail.getBedInfoList()) {
                if (ObjectUtils.isNotEmpty(bedInfoVO.getStudentId())) {
                    throw new ServiceException("当前宿舍中还存在入住学生，无法禁用！");
                }
            }
        }
        // 设置宿舍状态
        return new LambdaUpdateChainWrapper<>(baseMapper)
                .eq(DormitoryInfo::getDormitoryId, dormitoryId)
                .set(DormitoryInfo::getDormitoryStatus, status)
                .update();
    }

    /**
     * 批量设置床位（适用于不存在床位的宿舍）
     *
     * @param setBedDTO 设置床位DTO
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean setBed(SetBedDTO setBedDTO) {
        List<BedInfo> list = new LambdaQueryChainWrapper<>(bedInfoService.getBaseMapper())
                .in(BedInfo::getDormitoryId, setBedDTO.getDormitoryIds())
                .list();
        // 校验批量设置的宿舍中是否存在床位信息
        if (!CollectionUtils.isEmpty(list)) {
            List<DormitoryInfo> dormitoryInfoList = new LambdaQueryChainWrapper<>(baseMapper)
                    .in(DormitoryInfo::getDormitoryId, list.stream().map(BedInfo::getDormitoryId).collect(Collectors.toList()))
                    .list();
            // 抛出异常提示
            StringBuilder sb = new StringBuilder();
            sb.append("宿舍[");
            dormitoryInfoList.forEach(dormitoryInfo -> {
                sb.append(dormitoryInfo.getDormitoryName()).append(",");
            });
            sb.deleteCharAt(sb.length() - 1);
            sb.append("]存在床位信息，无法批量设置");
            throw new ServiceException(sb.toString());
        }
        // 批量生成床位信息
        List<BedInfo> bedInfoList = new ArrayList<>();
        setBedDTO.getDormitoryIds().forEach(dormitoryId -> {
            for (int i = 0; i < setBedDTO.getBedNum(); i++) {
                bedInfoList.add(
                        new BedInfo()
                                .setDormitoryId(dormitoryId)
                                .setBedStatus(BedStatusEnum.ENABLE.getCode())
                                .setBedName((i + 1) + "号床")
                );
            }
        });
        // 批量保存床位信息
        return bedInfoService.saveBatch(bedInfoList);
    }
}
