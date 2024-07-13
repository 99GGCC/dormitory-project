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
 * 公告信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "公告信息VO")
public class NoticeInfoVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 公告ID
     */
    @ApiModelProperty("公告ID")
    private Long noticeId;

    /**
     * 发布时间
     */
    @ApiModelProperty("发布时间")
    private Date noticeTime;

    /**
     * 公告标题
     */
    @ApiModelProperty("公告标题")
    private String noticeTitle;

    /**
     * 公告内容
     */
    @ApiModelProperty("公告内容")
    private String noticeContent;

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
