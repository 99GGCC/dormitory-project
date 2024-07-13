package com.dormitory.controller.qry;


import com.dormitory.common.Base;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 学院信息Qry
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
@ApiModel(value = "学院信息Qry")
public class CollegeInfoQry extends Base {

    private static final long serialVersionUID = 1L;

    /**
     * 学院名称
     */
    @ApiModelProperty("学院名称")
    private String collegeName;

}
