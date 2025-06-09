package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * <p>
 * 学院信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学院信息DTO")
public class CollegeInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 学院名称
     */
    @NotBlank(message = "学院名称")
    @ApiModelProperty("学院名称")
    private String collegeName;

    /**
     * 学院简介
     */
    @NotBlank(message = "学院简介")
    @ApiModelProperty("学院简介")
    private String collegeDesc;
}
