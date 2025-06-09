package com.dormitory.controller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * <p>
 * 调换申请DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "调换申请DTO")
public class ChangeApplyDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 学生ID
     */
    @ApiModelProperty("学生ID")
    private Long studentId;

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
     * 换入宿舍ID
     */
    @ApiModelProperty("换入宿舍ID")
    private Long inDormitoryId;

    /**
     * 换入床位ID
     */
    @ApiModelProperty("换入床位ID")
    private Long inBedId;

    /**
     * 申请原因
     */
    @ApiModelProperty("申请原因")
    private String applyReason;


}
