package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.dormitory.controller.vo.SignInBuildingVO;
import com.dormitory.entity.SignInBuilding;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 考勤宿舍表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SignInBuildingMapper extends BaseMapper<SignInBuilding> {

    /**
     * 根据楼栋ID查询签到宿舍列表
     *
     * @param buildingIds 楼栋IDs
     * @return List<SignInBuildingVO>
     */
    List<SignInBuildingVO> listByBuildingIds(@Param("buildingIds") List<Long> buildingIds);

    /**
     * 根据考勤ID查询考勤宿舍列表
     *
     * @param signInIds 考勤Ids
     * @return List<SignInBuildingVO>
     */
    List<SignInBuildingVO> listBySignInIds(@Param("signInIds") List<Long> signInIds);
}
