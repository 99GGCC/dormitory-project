package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 床位信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "床位信息DTO")
public class BedInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 床位名称
     */
    @ApiModelProperty("床位名称")
    private String bedName;

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
}
