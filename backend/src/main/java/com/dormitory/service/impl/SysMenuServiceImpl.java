package com.dormitory.service.impl;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.controller.qry.MenuQry;
import com.dormitory.controller.vo.MenuVO;
import com.dormitory.entity.SysMenu;
import com.dormitory.mapper.SysMenuMapper;
import com.dormitory.service.SysMenuService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.SpecialCharacterUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 菜单表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-05-07
 */
@Service
@RequiredArgsConstructor
public class SysMenuServiceImpl extends ServiceImpl<SysMenuMapper, SysMenu> implements SysMenuService {
    /**
     * 菜单分页查询
     *
     * @param menuQry 菜单查询Qry
     * @return IPage<MenuVO>
     */
    @Override
    public IPage<MenuVO> pageByQry(MenuQry menuQry) {
        IPage<SysMenu> pages = new Page<>(menuQry.getPage(), menuQry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotBlank(menuQry.getMenuName()),
                        SysMenu::getMenuName,
                        SpecialCharacterUtil.escapeStr(menuQry.getMenuName())
                ).eq(ObjectUtils.isNotEmpty(menuQry.getMenuType()) && !Constant.INTEGER_ZERO.equals(menuQry.getMenuType()),
                        SysMenu::getMenuType,
                        menuQry.getMenuType()
                ).page(pages);
        return CopyUtils.covertPage(pages, MenuVO.class);
    }

    /**
     * 菜单详情
     *
     * @param menuId 菜单ID
     * @return MenuVO
     */
    @Override
    public MenuVO detailById(Long menuId) {
        SysMenu menu = this.getById(menuId);
        return CopyUtils.classCopy(menu, MenuVO.class);
    }
}
