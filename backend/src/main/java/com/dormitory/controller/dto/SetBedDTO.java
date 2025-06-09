package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 设置床位DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel("设置床位DTO")
public class SetBedDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 宿舍IDs
     */
    @ApiModelProperty("宿舍IDs")
    private List<Long> dormitoryIds;

    /**
     * 床位数量
     */
    @ApiModelProperty("床位数量")
    private Integer bedNum;

}
