package com.dormitory.common;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 基础分页参数
 *
 * @author XXX
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Accessors(chain = true)
public class Base implements Serializable {

    private static final long serialVersionUID = 2683699207835074667L;

    /**
     * 当前页
     */
    private Integer page;

    /**
     * 分页大小
     */
    private Integer limit;

    /**
     * 开始数据
     */
    private Integer start;

    /**
     * 结束数据
     */
    private Integer end;

    /**
     * 构造器，设置不传分页的默认参数
     */
    public Base() {
        setParam();
    }

    /**
     * 设置查询分页默认值及参数
     */
    private void setParam() {
        if (this.page == null || this.page <= Constant.INTEGER_ZERO) {
            this.page = 1;
        }
        if (this.limit == null || this.limit <= Constant.INTEGER_ZERO) {
            this.limit = 10;
        }
        this.start = (this.page - Constant.INTEGER_ONE) * this.limit;
        this.end = this.start + this.limit;
    }
}
