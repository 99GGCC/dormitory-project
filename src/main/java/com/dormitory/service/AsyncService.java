package com.dormitory.service;

import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.entity.SignInIssue;

import java.util.List;

/**
 * 异步执行Service
 *
 * @author XXX
 */
public interface AsyncService {

    /**
     * 异步发送考勤邮件
     *
     * @param signInIssue 考勤信息
     * @param bedInfos    床位信息列表
     */
    void sendEmail(SignInIssue signInIssue, List<BedInfoVO> bedInfos);
}
