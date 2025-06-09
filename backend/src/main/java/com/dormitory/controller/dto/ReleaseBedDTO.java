package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 释放床位DTO
 *
 * @author XXX
 * @since 2024-07-24
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "释放床位DTO")
public class ReleaseBedDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

    /**
     * 动迁类型
     */
    @ApiModelProperty("动迁类型")
    private Integer relocationType;

}
