package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.controller.qry.RelocationRecordQry;
import com.dormitory.controller.vo.RelocationRecordVO;
import com.dormitory.entity.RelocationRecord;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * <p>
 * 动迁记录表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface RelocationRecordService extends IService<RelocationRecord> {

    /**
     * 动迁记录分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RelocationRecordVO>
     */
    IPage<RelocationRecordVO> pageByQry(RelocationRecordQry qry);
}
