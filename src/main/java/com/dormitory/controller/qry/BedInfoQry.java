package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;


/**
 * <p>
 * 床位信息Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "床位信息Qry")
public class BedInfoQry extends Base {

    private static final long serialVersionUID = 1L;


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
     * 是否使用
     */
    @ApiModelProperty("是否使用")
    private Integer isUse;
}
