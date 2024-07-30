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
 * 维修申请VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "维修申请VO")
public class RepairApplyVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 维修ID
     */
    @ApiModelProperty("维修ID")
    private Long repairId;

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
     * 维修设施
     */
    @ApiModelProperty("维修设施")
    private String repairFacilities;

    /**
     * 故障描述
     */
    @ApiModelProperty("故障描述")
    private String faultDescription;

    /**
     * 申请时间
     */
    @ApiModelProperty("申请时间")
    private Date applyTime;

    /**
     * 维修状态
     */
    @ApiModelProperty("维修状态")
    private Integer repairStatus;

    /**
     * 维修结果
     */
    @ApiModelProperty("维修结果")
    private Integer repairResult;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

    /**
     * 修改时间
     */
    @ApiModelProperty("修改时间")
    private Date updateTime;

}
