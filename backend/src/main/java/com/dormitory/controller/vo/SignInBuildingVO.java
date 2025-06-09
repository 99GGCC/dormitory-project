package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * <p>
 * 考勤宿舍VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤宿舍VO")
public class SignInBuildingVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 考勤宿舍ID
     */
    @ApiModelProperty("考勤宿舍ID")
    private Long signInBuildingId;

    /**
     * 考勤ID
     */
    @ApiModelProperty("考勤ID")
    private Long signInId;

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
     * 楼层
     */
    @ApiModelProperty("楼层")
    private Integer buildingFloor;

    /**
     * 楼栋类型1、男生宿舍 0、女生宿舍
     */
    @ApiModelProperty("楼栋类型1、男生宿舍 0、女生宿舍")
    private Integer buildingType;
}
