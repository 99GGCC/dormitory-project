package com.dormitory.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.RelocationRecordQry;
import com.dormitory.controller.vo.RelocationRecordVO;
import com.dormitory.entity.RelocationRecord;
import org.apache.ibatis.annotations.Param;

/**
 * <p>
 * 动迁记录表 Mapper 接口
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface RelocationRecordMapper extends BaseMapper<RelocationRecord> {

    /**
     * 动迁记录分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RelocationRecordVO>
     */
    IPage<RelocationRecordVO> pageByQry(@Param("qry") RelocationRecordQry qry, @Param("pages") Page<RelocationRecordVO> pages);
}
