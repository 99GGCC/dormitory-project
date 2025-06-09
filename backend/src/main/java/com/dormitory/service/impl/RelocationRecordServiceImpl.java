package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.dormitory.controller.qry.RelocationRecordQry;
import com.dormitory.controller.vo.RelocationRecordVO;
import com.dormitory.entity.RelocationRecord;
import com.dormitory.mapper.RelocationRecordMapper;
import com.dormitory.service.RelocationRecordService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 动迁记录表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
public class RelocationRecordServiceImpl extends ServiceImpl<RelocationRecordMapper, RelocationRecord> implements RelocationRecordService {

    /**
     * 动迁记录分页查询
     *
     * @param qry 查询Qry
     * @return IPage<RelocationRecordVO>
     */
    @Override
    public IPage<RelocationRecordVO> pageByQry(RelocationRecordQry qry) {
        return baseMapper.pageByQry(qry, new Page<>(qry.getPage(), qry.getLimit()));
    }
}
