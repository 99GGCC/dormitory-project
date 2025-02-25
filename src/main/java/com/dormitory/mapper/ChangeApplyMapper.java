package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.ChangeApplyQry;
import com.dormitory.controller.vo.ChangeApplyVO;
import com.dormitory.entity.ChangeApply;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 调换申请表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Mapper
public interface ChangeApplyMapper extends BaseMapper<ChangeApply> {

    /**
     * 调换申请分页查询
     *
     * @param qry   查询Qry
     * @param pages 分页类
     * @return IPage<ChangeApplyVO>
     */
    IPage<ChangeApplyVO> pageByQry(@Param("qry") ChangeApplyQry qry, @Param("pages") Page<ChangeApplyVO> pages);

    /**
     * 调换申请详情
     *
     * @param changeId 申请ID
     * @return ChangeApplyVO
     */
    ChangeApplyVO detailById(@Param("changeId") Long changeId);
}
