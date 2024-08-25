package com.dormitory.mapper;

import com.dormitory.controller.qry.BedInfoQry;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.entity.BedInfo;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * <p>
 * 床位信息表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface BedInfoMapper extends BaseMapper<BedInfo> {

    /**
     * 床位信息列表查询
     *
     * @param qry 查询Qry
     * @return List<BedInfoVO>·
     */
    List<BedInfoVO> listByQry(@Param("qry") BedInfoQry qry);
}
