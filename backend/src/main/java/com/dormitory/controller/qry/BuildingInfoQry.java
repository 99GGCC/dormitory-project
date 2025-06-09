package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;


/**
 * <p>
 * 楼栋信息Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "楼栋信息Qry")
public class BuildingInfoQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 楼栋名称
     */
    @ApiModelProperty("楼栋名称")
    private String buildingName;

    /**
     * 楼栋类型1、男生宿舍 0、女生宿舍
     */
    @ApiModelProperty("楼栋类型1、男生宿舍 0、女生宿舍")
    private Integer buildingType;

    /**
     * 楼层管理员
     */
    @ApiModelProperty("楼层管理员")
    private String buildingAdmin;

}
