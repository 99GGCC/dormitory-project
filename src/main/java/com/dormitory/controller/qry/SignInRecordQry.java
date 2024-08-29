package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;


/**
 * <p>
 * 考勤记录Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤记录Qry")
public class SignInRecordQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 考勤ID
     */
    @ApiModelProperty("考勤ID")
    private Long signInId;

    /**
     * 学生ID
     */
    @ApiModelProperty("学生ID")
    private Long studentId;

    /**
     * 考勤状态
     */
    @ApiModelProperty("考勤状态")
    private Integer recordStatus;
}
