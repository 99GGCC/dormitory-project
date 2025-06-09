package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;


/**
 * <p>
 * 宿舍信息Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "宿舍信息Qry")
public class DormitoryInfoQry extends Base {

    private static final long serialVersionUID = 1L;

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
}
