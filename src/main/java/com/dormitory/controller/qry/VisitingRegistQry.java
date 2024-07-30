package com.dormitory.controller.qry;


import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.util.Date;

/**
 * <p>
 * 来访登记Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "来访登记Qry")
public class VisitingRegistQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 来访姓名
     */
    @ApiModelProperty("来访姓名")
    private String visitingName;

    /**
     * 来访缘由
     */
    @ApiModelProperty("来访缘由")
    private String visitingReason;

    /**
     * 值班人员
     */
    @ApiModelProperty("值班人员")
    private Long adminId;


}
