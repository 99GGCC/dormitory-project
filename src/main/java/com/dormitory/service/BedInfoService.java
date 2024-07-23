package com.dormitory.service;

import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.BedInfoDTO;
import com.dormitory.entity.BedInfo;

/**
 * <p>
 * 床位信息表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface BedInfoService extends IService<BedInfo> {

    /**
     * 新增床位信息
     *
     * @param dto 床位DTO
     * @return Boolean
     */
    Boolean add(BedInfoDTO dto);

    /**
     * 编辑床位信息
     *
     * @param bedId 床位ID
     * @param dto   床位DTO
     * @return Boolean
     */
    Boolean edit(Long bedId, BedInfoDTO dto);

    /**
     * 删除床位信息
     *
     * @param bedId 床位ID
     * @return Boolean
     */
    Boolean del(Long bedId);
}
