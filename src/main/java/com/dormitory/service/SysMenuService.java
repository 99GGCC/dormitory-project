package com.dormitory.service;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.dormitory.controller.qry.MenuQry;
import com.dormitory.controller.vo.MenuVO;
import com.dormitory.entity.SysMenu;

/**
 * <p>
 * 菜单表 服务类
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
public interface SysMenuService extends IService<SysMenu> {
    /**
     * 菜单分页查询
     *
     * @param menuQry 菜单查询Qry
     * @return IPage<MenuVO>
     */
    IPage<MenuVO> pageByQry(MenuQry menuQry);

    /**
     * 菜单详情
     *
     * @param menuId 菜单ID
     * @return MenuVO
     */
    MenuVO detailById(Long menuId);
}
