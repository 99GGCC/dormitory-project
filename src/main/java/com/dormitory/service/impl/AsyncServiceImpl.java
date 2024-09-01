package com.dormitory.service.impl;

import com.baomidou.mybatisplus.extension.conditions.update.LambdaUpdateChainWrapper;
import com.dormitory.common.BooleanEnum;
import com.dormitory.common.SignInRecordStatusEnum;
import com.dormitory.config.EmailConfig;
import com.dormitory.controller.vo.BedInfoVO;
import com.dormitory.entity.SignInIssue;
import com.dormitory.entity.SignInRecord;
import com.dormitory.service.AsyncService;
import com.dormitory.service.SignInRecordService;
import com.dormitory.utils.IdUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.mail.HtmlEmail;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.net.InetAddress;
import java.util.ArrayList;
import java.util.List;

/**
 * 异步执行Service实现
 *
 * @author XXX
 */
@Slf4j
@Component
@RequiredArgsConstructor
@EnableAsync
public class AsyncServiceImpl implements AsyncService {

    /**
     * 考勤记录Service
     */
    private final SignInRecordService signInRecordService;

    /**
     * 邮箱配置
     */
    private final EmailConfig emailConfig;

    /**
     * 异步发送考勤邮件
     *
     * @param signInIssue 考勤信息
     * @param bedInfos    床位信息列表
     */
    @Override
    @Async("threadPoolTaskExecutor")
    public void sendEmail(SignInIssue signInIssue, List<BedInfoVO> bedInfos) {
        List<SignInRecord> signInRecords = new ArrayList<>();
        for (BedInfoVO bedInfo : bedInfos) {
            Long recordId = IdUtils.getLongId();
            signInRecords.add(
                    new SignInRecord()
                            .setRecordId(recordId)
                            .setSignInId(signInIssue.getSignInId())
                            .setStudentId(bedInfo.getStudentId())
                            .setRecordStatus(SignInRecordStatusEnum.NOT_SIGN.getCode())
                            .setSendStatus(BooleanEnum.FALSE.getCode())
            );
            // 设置考勤记录ID（发送邮件改变状态用）
            bedInfo.setRecordId(recordId);
        }
        // 批量保存签到
        signInRecordService.saveBatch(signInRecords);
        // 循环签到链接
        for (BedInfoVO bedInfoVO : bedInfos) {
            try {
                // 获取项目启动IP
                InetAddress inetAddress = InetAddress.getLocalHost();
                // 生成考勤签到链接
                String link = "http://" + inetAddress.getHostAddress() + ":" + emailConfig.getStudentPort() + "/#/home?recordId=" + signInIssue.getSignInId() +
                        "&studentId=" + bedInfoVO.getStudentId();
                // 异步发送邮件
                boolean status = sendEmail(bedInfoVO.getStudentEmail(), "考勤签到", "考勤签到", link);
                // 判断是否发送成功
                if (status) {
                    new LambdaUpdateChainWrapper<>(signInRecordService.getBaseMapper())
                            .eq(SignInRecord::getRecordId, bedInfoVO.getRecordId())
                            .set(SignInRecord::getSendStatus, BooleanEnum.TRUE.getCode())
                            .update();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public Boolean sendEmail(String email, String subject, String mess, String link) {
        mess = "<html><body><p>" + mess + "</p><p>点击以下链接进行签到: <a href='" + link + "'>签到链接</a></p></body></html>";
        try {
            HtmlEmail emails = new HtmlEmail();
            emails.setHostName(emailConfig.getHost());
            emails.setCharset("utf-8");
            emails.setSmtpPort(emailConfig.getPort());
            emails.setSSLOnConnect(true);
            emails.addTo(email);//设置收件人
            emails.setFrom(emailConfig.getForm(), emailConfig.getName());
            emails.setAuthentication(emailConfig.getForm(), emailConfig.getAuthentication());
            //设置发送主题
            emails.setSubject(subject);
            //设置发送内容
            emails.setMsg(mess);
            //进行发送
            emails.send();
        } catch (Exception e) {
            return false;
        }
        return true;
    }
}
