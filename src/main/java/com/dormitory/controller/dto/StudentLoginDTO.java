package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * 学生登录DTO
 *
 * @author XXX
 * @since 2024-07-31
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生登录DTO")
public class StudentLoginDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 学生学号
     */
    @NotBlank(message = "学生学号")
    @ApiModelProperty(value = "学生学号")
    private String studentNum;

    /**
     * 学生密码
     */
    @NotBlank(message = "学生密码")
    @ApiModelProperty(value = "学生密码")
    private String studentPass;

    /**
     * 验证码
     */
    @NotBlank(message = "验证码")
    @ApiModelProperty(value = "验证码")
    private String code;
}
