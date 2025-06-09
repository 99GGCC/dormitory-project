package com.dormitory.controller.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.List;


/**
 * <p>
 * 考勤发布DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤发布DTO")
public class SignInIssueDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 考勤截止时间
     */
    @NotNull(message = "考勤截止时间")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty("考勤截止时间")
    private Date endTime;

    /**
     * 考勤楼栋Ids
     */
    @NotNull(message = "考勤楼栋Ids")
    @NotEmpty(message = "考勤楼栋Ids")
    @ApiModelProperty("考勤楼栋Ids")
    private List<Long> buildingIds;

}
