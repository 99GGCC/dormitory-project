package com.dormitory.controller.dto;


import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import java.io.Serializable;

/**
 * <p>
 * 公告信息DTO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "公告信息DTO")
public class NoticeInfoDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 公告标题
     */
    @NotBlank(message = "公告标题")
    @ApiModelProperty("公告标题")
    private String noticeTitle;

    /**
     * 公告内容
     */
    @NotBlank(message = "公告内容")
    @ApiModelProperty("公告内容")
    private String noticeContent;

}
