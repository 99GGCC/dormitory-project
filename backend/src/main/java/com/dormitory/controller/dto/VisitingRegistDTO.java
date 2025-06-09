package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 来访登记DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "来访登记DTO")
public class VisitingRegistDTO implements Serializable {

    private static final long serialVersionUID = 1L;

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

}
