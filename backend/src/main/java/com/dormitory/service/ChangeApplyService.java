package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.ChangeApplyDTO;
import com.dormitory.controller.dto.ChangeApplyStatusDTO;
import com.dormitory.controller.qry.ChangeApplyQry;
import com.dormitory.controller.vo.ChangeApplyVO;
import com.dormitory.entity.ChangeApply;

/**
 * <p>
 * 调换申请表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface ChangeApplyService extends IService<ChangeApply> {

    /**
     * 调换申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ChangeApplyVO>
     */
    IPage<ChangeApplyVO> pageByQry(ChangeApplyQry qry);

    /**
     * 调换申请详情
     *
     * @param changeId 申请ID
     * @return ChangeApplyVO
     */
    ChangeApplyVO detailById(Long changeId);

    /**
     * 发起调换申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    Boolean initiate(ChangeApplyDTO dto);

    /**
     * 处理调换申请
     *
     * @param changeId 申请ID
     * @param dto      调换结果DTO
     * @return Boolean
     */
    Boolean status(Long changeId, ChangeApplyStatusDTO dto);

    /**
     * 学生调换申请分页查询
     *
     * @param qry 查询Qry
     * @return IPage<ChangeApplyVO>
     */
    IPage<ChangeApplyVO> studentPageByQry(ChangeApplyQry qry);

    /**
     * 学生发起调换申请
     *
     * @param dto 申请DTO
     * @return Boolean
     */
    Boolean studentInitiate(ChangeApplyDTO dto);

    /**
     * 学生取消调换申请
     *
     * @param changeId 申请ID
     * @return Boolean
     */
    Boolean cancel(Long changeId);
}
