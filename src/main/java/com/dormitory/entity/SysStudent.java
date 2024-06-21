package com.dormitory.entity;



import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;

import java.io.Serializable;
import java.util.Date;

import com.baomidou.mybatisplus.annotation.TableId;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 学生信息表
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class SysStudent implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 学生ID
     */
    @TableId
    private Long studentId;

    /**
     * 学生姓名
     */
    private String studentName;

    /**
     * 学生学号
     */
    private String studentNum;

    /**
     * 手机号码
     */
    private String studentPhone;

    /**
     * 学生邮箱
     */
    private String studentEmail;

    /**
     * 登录密码
     */
    private String studentPass;

    /**
     * 加密盐
     */
    private String studentSalt;

    /**
     * 学生性别：0、女 1、男
     */
    private Integer studentSex;

    /**
     * 班级ID
     */
    private Long classesId;

    /**
     * 宿舍ID
     */
    private Long dormitoryId;

    /**
     * 床位ID
     */
    private Long bedId;

    /**
     * 学生状态
     */
    private Integer studentStatus;

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
