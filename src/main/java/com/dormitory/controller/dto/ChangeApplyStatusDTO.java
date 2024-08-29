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
 * 来访登记DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "调换申请结果DTO")
public class ChangeApplyStatusDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 状态
     */
    @NotNull(message = "状态")
    @ApiModelProperty("状态")
    private Integer status;

    /**
     * 审核结果
     */
    @NotBlank(message = "审核结果")
    @ApiModelProperty("审核结果")
    private String applyResult;

}
