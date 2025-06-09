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
 * 学生个人信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生个人信息DTO")
public class StudentMineDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 手机号码
     */
    @NotBlank(message = "手机号码")
    @ApiModelProperty("手机号码")
    private String studentPhone;

    /**
     * 学生邮箱
     */
    @NotBlank(message = "学生邮箱")
    @ApiModelProperty("学生邮箱")
    private String studentEmail;
}
