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
 * 班级信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "班级信息DTO")
public class ClassesInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 班级名称
     */
    @NotBlank(message = "班级名称")
    @ApiModelProperty("班级名称")
    private String classesName;

    /**
     * 辅导员姓名
     */
    @NotBlank(message = "辅导员姓名")
    @ApiModelProperty("辅导员姓名")
    private String instructorName;

    /**
     * 联系电话
     */
    @NotBlank(message = "联系电话")
    @ApiModelProperty("联系电话")
    private String instructorPhone;

    /**
     * 专业ID
     */
    @NotNull(message = "专业ID")
    @ApiModelProperty("专业ID")
    private Long majorId;

}
