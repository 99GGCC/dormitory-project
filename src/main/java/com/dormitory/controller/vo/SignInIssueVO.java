package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;


/**
 * <p>
 * 考勤发布VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "考勤发布VO")
public class SignInIssueVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 考勤ID
     */
    @ApiModelProperty("考勤ID")
    private Long signInId;

    /**
     * 发布时间
     */
    @ApiModelProperty("发布时间")
    private Date issueTime;

    /**
     * 发布人
     */
    @ApiModelProperty("发布人")
    private Long adminId;

    /**
     * 考勤总人数
     */
    @ApiModelProperty("考勤总人数")
    private Integer totalStudent;

    /**
     * 实际签到人数
     */
    @ApiModelProperty("实际签到人数")
    private Integer realityStudent;

    /**
     * 考勤截止时间
     */
    @ApiModelProperty("考勤截止时间")
    private Date endTime;

    /**
     * 考勤状态
     */
    @ApiModelProperty("考勤状态")
    private Integer signInStatus;

    /**
     * 考勤宿舍列表
     */
    @ApiModelProperty("考勤宿舍列表")
    private List<SignInBuildingVO> buildingVOList;

}
