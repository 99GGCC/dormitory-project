package com.dormitory.controller.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * <p>
 * 字典信息VO
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "字典信息VO")
public class SysDictVO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 字典ID
     */
    @ApiModelProperty("字典ID")
    private Long dictId;

    /**
     * 字典类型编码
     */
    @ApiModelProperty("字典类型编码")
    private String dictTypeCode;

    /**
     * 字典类型名称
     */
    @ApiModelProperty("字典类型名称")
    private String dictTypeName;

    /**
     * 字典编码
     */
    @ApiModelProperty("字典编码")
    private String dictCode;

    /**
     * 字典名称
     */
    @ApiModelProperty("字典名称")
    private String dictName;

    /**
     * 字典值
     */
    @ApiModelProperty("字典值")
    private String dictValue;

}
