package com.dormitory.mapper;

import com.dormitory.controller.vo.DormitoryInfoVO;
import com.dormitory.entity.DormitoryInfo;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 宿舍信息表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface DormitoryInfoMapper extends BaseMapper<DormitoryInfo> {

    /**
     * 宿舍信息详情
     *
     * @param dormitoryId 宿舍ID
     * @return DormitoryInfoVO
     */
    DormitoryInfoVO detail(@Param("dormitoryId") Long dormitoryId);
}
