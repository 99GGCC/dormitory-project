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
 * 学生信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生信息DTO")
public class StudentDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 学生姓名
     */
    @NotBlank(message = "学生姓名")
    @ApiModelProperty("学生姓名")
    private String studentName;

    /**
     * 学生学号
     */
    @NotBlank(message = "学生学号")
    @ApiModelProperty("学生学号")
    private String studentNum;

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

    /**
     * 学生性别：0、女 1、男
     */
    @NotNull(message = "学生性别")
    @ApiModelProperty("学生性别：0、女 1、男")
    private Integer studentSex;

    /**
     * 宿舍ID
     */
    @ApiModelProperty("宿舍ID")
    private Long dormitoryId;

    /**
     * 床位ID
     */
    @ApiModelProperty("床位ID")
    private Long bedId;

    /**
     * 班级ID
     */
    @NotBlank(message = "班级ID")
    @ApiModelProperty("班级ID")
    private Long classesId;
}
