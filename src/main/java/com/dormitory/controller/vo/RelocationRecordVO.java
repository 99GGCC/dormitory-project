package com.dormitory.controller.vo;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 动迁记录VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "动迁记录VO")
public class RelocationRecordVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 记录ID
     */
    @ApiModelProperty("记录ID")
    private Long relocationId;

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
     * 楼栋类型1、男生宿舍 0、女生宿舍
     */
    @ApiModelProperty("楼栋类型1、男生宿舍 0、女生宿舍")
    private Integer buildingType;

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
     * 床位状态：1、启用 0、禁用
     */
    @ApiModelProperty("床位状态：1、启用 0、禁用")
    private Integer bedStatus;

    /**
     * 是否宿舍长
     */
    @ApiModelProperty("是否宿舍长")
    private Integer isHead;

    /**
     * 记录时间
     */
    @ApiModelProperty("记录时间")
    private Date relocationTime;

    /**
     * 动迁类型
     */
    @ApiModelProperty("动迁类型")
    private Integer relocationType;


}
