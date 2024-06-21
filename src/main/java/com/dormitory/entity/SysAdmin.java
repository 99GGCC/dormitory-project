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
 * 管理员表
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class SysAdmin implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 管理员ID
     */
    @TableId
    private Long adminId;

    /**
     * 管理员名称
     */
    private String adminName;

    /**
     * 手机号码
     */
    private String adminPhone;

    /**
     * 登录密码
     */
    private String adminPass;

    /**
     * 加密盐
     */
    private String adminSalt;

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
