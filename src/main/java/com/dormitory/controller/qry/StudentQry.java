package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;


/**
 * <p>
 * 学生信息Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学生信息Qry")
public class StudentQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 学生姓名
     */
    @ApiModelProperty("学生姓名")
    private String studentName;

    /**
     * 学生学号
     */
    @ApiModelProperty("学生学号")
    private String studentNum;

    /**
     * 班级Id
     */
    @ApiModelProperty("班级Id")
    private Long classesId;

    /**
     * 专业ID
     */
    @ApiModelProperty("专业ID")
    private Long majorId;

    /**
     * 学院ID
     */
    @ApiModelProperty("学院ID")
    private Long collegeId;

    /**
     * 学生状态
     */
    @ApiModelProperty("学生状态")
    private Integer studentStatus;
}
