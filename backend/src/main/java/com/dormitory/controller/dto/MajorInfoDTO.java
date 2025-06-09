package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 专业信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "专业信息DTO")
public class MajorInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 专业名称
     */
    @NotBlank(message = "专业名称")
    @ApiModelProperty("专业名称")
    private String majorName;

    /**
     * 专业简介
     */
    @NotBlank(message = "专业简介")
    @ApiModelProperty("专业简介")
    private String majorDesc;

    /**
     * 学院ID
     */
    @NotNull(message = "学院ID")
    @ApiModelProperty("学院ID")
    private Long collegeId;

}
