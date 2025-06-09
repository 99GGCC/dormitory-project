package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 安排床位DTO
 *
 * @author XXX
 * @since 2024-07-24
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "安排床位DTO")
public class ArrangeBedDTO implements Serializable {
    private static final long serialVersionUID = 1L;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 使用学生
     */
    @ApiModelProperty("使用学生")
    private Long useStudent;

    /**
     * 是否宿舍长
     */
    @ApiModelProperty("是否宿舍长")
    private Integer isHead;
}
