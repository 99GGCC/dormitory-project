package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.qry.SignInRecordQry;
import com.dormitory.controller.vo.SignInRecordVO;
import com.dormitory.entity.SignInRecord;

/**
 * <p>
 * 考勤记录表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
public interface SignInRecordService extends IService<SignInRecord> {

    /**
     * 考勤记录分页查询
     *
     * @param qry 查询Qry
     * @return IPage<SignInRecordVO>
     */
    IPage<SignInRecordVO> pageByQry(SignInRecordQry qry);

    /**
     * 给学生签到
     *
     * @param recordId 记录ID
     * @return Boolean
     */
    Boolean sign(Long recordId);

    /**
     * 学生签到
     *
     * @param recordId 记录ID
     * @return Boolean
     */
    Boolean studentSign(Long recordId);

    /**
     * 考勤记录详情
     *
     * @param recordId 记录ID
     * @return SignInRecordVO
     */
    SignInRecordVO detailById(Long recordId);
}
