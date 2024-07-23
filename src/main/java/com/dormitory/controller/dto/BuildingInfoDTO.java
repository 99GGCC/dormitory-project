package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * <p>
 * 楼栋信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "楼栋信息DTO")
public class BuildingInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

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

    /**
     * 楼层房间数量
     */
    @ApiModelProperty("楼层房间数量")
    private Integer roomNum;

    /**
     * 楼层管理员
     */
    @ApiModelProperty("楼层管理员")
    private String buildingAdmin;

    /**
     * 联系电话
     */
    @ApiModelProperty("联系电话")
    private String buildingPhone;

}
