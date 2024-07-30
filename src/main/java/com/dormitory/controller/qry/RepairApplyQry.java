package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;


/**
 * <p>
 * 维修申请Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "维修申请Qry")
public class RepairApplyQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 学生ID
     */
    @ApiModelProperty("学生ID")
    private Long studentId;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 楼栋ID
     */
    @ApiModelProperty("楼栋ID")
    private Long buildingId;

    /**
     * 维修状态
     */
    @ApiModelProperty("维修状态")
    private Integer repairStatus;

}
