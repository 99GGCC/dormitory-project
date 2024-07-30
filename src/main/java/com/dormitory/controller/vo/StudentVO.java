package com.dormitory.controller.vo;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 学生信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生信息VO")
public class StudentVO implements Serializable {

    private static final long serialVersionUID = 1L;

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
     * 学生邮箱
     */
    @ApiModelProperty("学生邮箱")
    private String studentEmail;

    /**
     * 学生性别：0、女 1、男
     */
    @ApiModelProperty("学生性别：0、女 1、男")
    private Integer studentSex;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 宿舍名称
     */
    @ApiModelProperty("宿舍名称")
    private String dormitoryName;

    /**
     * 楼层
     */
    @ApiModelProperty("楼层")
    private Integer buildingFloor;

    /**
     * 楼栋ID
     */
    @ApiModelProperty("楼栋ID")
    private Long buildingId;

    /**
     * 楼栋名称
     */
    @ApiModelProperty("楼栋名称")
    private String buildingName;

    /**
     * 楼栋简称
     */
    @ApiModelProperty("楼栋简称")
    private String shortName;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

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
