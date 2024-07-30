package com.dormitory.controller.vo;

import cn.dev33.satoken.stp.SaTokenInfo;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * 学生登录VO
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生登录VO")
public class StudentLoginVO {

    /**
     * 学生ID
     */
    private Long studentId;

    /**
     * 学生姓名
     */
    private String studentName;

    /**
     * 学生学号
     */
    private String studentNum;

    /**
     * 手机号码
     */
    private String studentPhone;

    /**
     * 学生邮箱
     */
    private String studentEmail;

    /**
     * 学生性别：0、女 1、男
     */
    private Integer studentSex;

    /**
     * 班级ID
     */
    private Long classesId;

    /**
     * 宿舍ID
     */
    private Long dormitoryId;

    /**
     * 床位ID
     */
    private Long bedId;

    /**
     * 学生状态
     */
    private Integer studentStatus;

    /**
     * saToken
     */
    @ApiModelProperty("saToken")
    private String saToken;

    /**
     * 用户登录信息
     */
    @ApiModelProperty("用户登录信息")
    private SaTokenInfo tokenInfo;
}
