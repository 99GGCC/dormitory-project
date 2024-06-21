package com.dormitory.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;


/**
 * <p>
 * 维修申请表
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class RepairApply implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 维修ID
     */
    @TableId
    private Long repairId;

    /**
     * 学生ID
     */
    private Long studentId;

    /**
     * 宿舍ID
     */
    private Long dormitoryId;

    /**
     * 维修设施
     */
    private String repairFacilities;

    /**
     * 故障描述
     */
    private String faultDescription;

    /**
     * 申请时间
     */
    private Date applyTime;

    /**
     * 维修状态
     */
    private Integer repairStatus;

    /**
     * 维修结果
     */
    private Integer repairResult;

    /**
     * 删除标志，0未删除 1删除
     */
    @TableField(fill = FieldFill.INSERT)
    private Integer deleteFlag;

    /**
     * 创建者ID
     */
    @TableField(fill = FieldFill.INSERT)
    private Long createId;

    /**
     * 创建者名称
     */
    @TableField(fill = FieldFill.INSERT)
    private String createName;

    /**
     * 创建时间
     */
    @TableField(fill = FieldFill.INSERT)
    private Date createTime;

    /**
     * 修改者ID
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updateId;

    /**
     * 修改者名称
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private String updateName;

    /**
     * 修改时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateTime;


}
