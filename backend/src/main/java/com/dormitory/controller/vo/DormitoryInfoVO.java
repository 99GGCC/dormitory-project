package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;


/**
 * <p>
 * 宿舍信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "宿舍信息VO")
public class DormitoryInfoVO implements Serializable {

    private static final long serialVersionUID = 1L;

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
     * 楼栋ID
     */
    @ApiModelProperty("楼栋ID")
    private Long buildingId;

    /**
     * 楼层
     */
    @ApiModelProperty("楼层")
    private Integer buildingFloor;

    /**
     * 宿舍状态：1、启用 0、禁用
     */
    @ApiModelProperty("宿舍状态：1、启用 0、禁用")
    private Integer dormitoryStatus;

    /**
     * 使用状态：1、已使用 0、未使用
     */
    @ApiModelProperty("使用状态：1、已使用 0、未使用")
    private Integer useStatus;

    /**
     * 床位列表
     */
    @ApiModelProperty("床位列表")
    private List<BedInfoVO> bedInfoList;
}
