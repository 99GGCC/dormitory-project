package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.dto.SignInIssueDTO;
import com.dormitory.controller.qry.SignInIssueQry;
import com.dormitory.controller.vo.SignInIssueVO;
import com.dormitory.entity.SignInIssue;

/**
 * <p>
 * 考勤发布表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SignInIssueService extends IService<SignInIssue> {

    /**
     * 考勤信息分页查询
     *
     * @param qry qry
     * @return IPage<SignInIssueVO>
     */
    IPage<SignInIssueVO> pageByQry(SignInIssueQry qry);

    /**
     * 考勤信息详情
     *
     * @param signInId 考勤ID
     * @return SignInIssueVO
     */
    SignInIssueVO detailById(Long signInId);

    /**
     * 考勤信息发布
     *
     * @param dto 发布DTO
     * @return Boolean
     */
    Boolean add(SignInIssueDTO dto);

    /**
     * 考勤信息作废
     *
     * @param signInId 考勤ID
     * @return Boolean
     */
    Boolean status(Long signInId);
}
