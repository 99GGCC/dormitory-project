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
 * 调换申请VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "调换申请VO")
public class ChangeApplyVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 申请ID
     */
    @ApiModelProperty("申请ID")
    private Long changeId;

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
     * 换入宿舍ID
     */
    @ApiModelProperty("换入宿舍ID")
    private Long inDormitoryId;

    /**
     * 换入宿舍名称
     */
    @ApiModelProperty("换入宿舍名称")
    private String inDormitoryName;

    /**
     * 换入楼层
     */
    @ApiModelProperty("换入楼层")
    private Integer inBuildingFloor;

    /**
     * 换入楼栋ID
     */
    @ApiModelProperty("换入楼栋ID")
    private Long inBuildingId;

    /**
     * 换入楼栋名称
     */
    @ApiModelProperty("换入楼栋名称")
    private String inBuildingName;

    /**
     * 换入床位ID
     */
    @ApiModelProperty("换入床位ID")
    private Long inBedId;

    /**
     * 换入床位名称
     */
    @ApiModelProperty("换入床位名称")
    private String inBedName;

    /**
     * 申请时间
     */
    @ApiModelProperty("申请时间")
    private Date applyTime;

    /**
     * 申请原因
     */
    @ApiModelProperty("申请原因")
    private String applyReason;

    /**
     * 申请状态
     */
    @ApiModelProperty("申请状态")
    private Integer applyStatus;

    /**
     * 申请结果
     */
    @ApiModelProperty("申请结果")
    private String applyResult;


}
