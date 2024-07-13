package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;


/**
 * <p>
 * 班级信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "班级信息VO")
public class ClassesInfoVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 班级ID
     */
    @ApiModelProperty("班级ID")
    private Long classesId;

    /**
     * 班级名称
     */
    @ApiModelProperty("班级名称")
    private String classesName;

    /**
     * 辅导员姓名
     */
    @ApiModelProperty("辅导员姓名")
    private String instructorName;

    /**
     * 联系电话
     */
    @ApiModelProperty("联系电话")
    private String instructorPhone;

    /**
     * 专业ID
     */
    @ApiModelProperty("专业ID")
    private Long majorId;

    /**
     * 班级状态
     */
    @ApiModelProperty("班级状态")
    private Integer classesStatus;

    /**
     * 创建者ID
     */
    @ApiModelProperty("创建者ID")
    private Long createId;

    /**
     * 创建者名称
     */
    @ApiModelProperty("创建者名称")
    private String createName;

    /**
     * 创建时间
     */
    @ApiModelProperty("创建时间")
    private Date createTime;

    /**
     * 修改者ID
     */
    @ApiModelProperty("修改者ID")
    private Long updateId;

    /**
     * 修改者名称
     */
    @ApiModelProperty("修改者名称")
    private String updateName;

    /**
     * 修改时间
     */
    @ApiModelProperty("修改时间")
    private Date updateTime;

}
