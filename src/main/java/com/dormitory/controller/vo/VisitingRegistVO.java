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
 * 来访登记VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "来访登记VO")
public class VisitingRegistVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 登记ID
     */
    @ApiModelProperty("登记ID")
    private Long registId;

    /**
     * 登记时间
     */
    @ApiModelProperty("登记时间")
    private Date registTime;

    /**
     * 来访姓名
     */
    @ApiModelProperty("来访姓名")
    private String visitingName;

    /**
     * 来访电话
     */
    @ApiModelProperty("来访电话")
    private String visitingPhone;

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

    /**
     * 管理员名称
     */
    @ApiModelProperty("管理员名称")
    private String createName;


}
