package com.dormitory.controller.qry;

import com.dormitory.common.Base;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.util.Date;
import java.util.List;


/**
 * <p>
 * 考勤发布Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤发布Qry")
public class SignInIssueQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 考勤截止时间开始
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty("考勤截止时间开始")
    private Date endTimeStart;

    /**
     * 考勤截止时间结束
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty("考勤截止时间结束")
    private Date endTimeEnd;

    /**
     * 考勤宿舍列表
     */
    @ApiModelProperty("考勤宿舍列表")
    private List<Long> buildingIds;

}
