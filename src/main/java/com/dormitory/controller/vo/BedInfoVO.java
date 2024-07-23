package com.dormitory.controller.vo;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 床位信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "床位信息VO")
public class BedInfoVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

    /**
     * 床位名称
     */
    @ApiModelProperty("床位名称")
    private String bedName;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 床位状态：1、启用 0、禁用
     */
    @ApiModelProperty("床位状态：1、启用 0、禁用")
    private Integer bedStatus;

    /**
     * 使用学生
     */
    @ApiModelProperty("使用学生")
    private Long useStudent;

    /**
     * 学生ID
     */
    @ApiModelProperty("学生ID")
    private Long studentId;

    /**
     * 学生姓名
     */
    @ApiModelProperty("学生姓名")
    private String studentName;

    /**
     * 学生学号
     */
    @ApiModelProperty("学生学号")
    private String studentNum;

    /**
     * 手机号码
     */
    @ApiModelProperty("手机号码")
    private String studentPhone;

    /**
     * 学生性别：0、女 1、男
     */
    @ApiModelProperty("学生性别：0、女 1、男")
    private Integer studentSex;

    /**
     * 学生状态
     */
    @ApiModelProperty("学生状态")
    private Integer studentStatus;

    /**
     * 班级ID
     */
    @ApiModelProperty("班级ID")
    private Long classesId;

    /**
     * 班级名称
     */
    @ApiModelProperty("班级名称")
    private String classesName;

    /**
     * 班级状态
     */
    @ApiModelProperty("班级状态")
    private Integer classesStatus;

    /**
     * 专业ID
     */
    @ApiModelProperty("专业ID")
    private Long majorId;

    /**
     * 专业名称
     */
    @ApiModelProperty("专业名称")
    private String majorName;

    /**
     * 学院ID
     */
    @ApiModelProperty("学院ID")
    private Long collegeId;

    /**
     * 学院名称
     */
    @ApiModelProperty("学院名称")
    private String collegeName;
}
