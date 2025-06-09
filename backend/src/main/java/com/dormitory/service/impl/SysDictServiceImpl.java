package com.dormitory.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.conditions.query.LambdaQueryChainWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.dormitory.common.Constant;
import com.dormitory.controller.dto.SysDictDTO;
import com.dormitory.controller.qry.SysDictQry;
import com.dormitory.controller.vo.SysDictVO;
import com.dormitory.entity.SysDict;
import com.dormitory.exception.ServiceException;
import com.dormitory.mapper.SysDictMapper;
import com.dormitory.service.SysDictService;
import com.dormitory.utils.CopyUtils;
import com.dormitory.utils.RedisUtil;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * <p>
 * 字典表 服务实现类
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@Service
@RequiredArgsConstructor
public class SysDictServiceImpl extends ServiceImpl<SysDictMapper, SysDict> implements SysDictService {

    /**
     * redis工具
     */
    private final RedisUtil redisUtil;

    /**
     * 字典信息分页查询
     *
     * @param qry 查询qry
     * @return IPage<SysDictVO>
     */
    @Override
    public IPage<SysDictVO> pageByQry(SysDictQry qry) {
        IPage<SysDict> pages = new Page<>(qry.getPage(), qry.getLimit());
        pages = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getDictTypeCode()), SysDict::getDictTypeCode, qry.getDictTypeCode())
                .like(StringUtils.isNotEmpty(qry.getDictTypeName()), SysDict::getDictTypeName, qry.getDictTypeName())
                .like(StringUtils.isNotEmpty(qry.getDictCode()), SysDict::getDictCode, qry.getDictCode())
                .like(StringUtils.isNotEmpty(qry.getDictName()), SysDict::getDictName, qry.getDictName())
                .page(pages);
        return CopyUtils.covertPage(pages, SysDictVO.class);
    }

    /**
     * 字典信息列表查询
     *
     * @param qry 查询qry
     * @return List<SysDictVO>
     */
    @Override
    public List<SysDictVO> listByQry(SysDictQry qry) {
        List<SysDict> list = new LambdaQueryChainWrapper<>(baseMapper)
                .like(StringUtils.isNotEmpty(qry.getDictTypeCode()), SysDict::getDictTypeCode, qry.getDictTypeCode())
                .like(StringUtils.isNotEmpty(qry.getDictTypeName()), SysDict::getDictTypeName, qry.getDictTypeName())
                .like(StringUtils.isNotEmpty(qry.getDictCode()), SysDict::getDictCode, qry.getDictCode())
                .like(StringUtils.isNotEmpty(qry.getDictName()), SysDict::getDictName, qry.getDictName())
                .list();
        return CopyUtils.classCopyList(list, SysDictVO.class);
    }

    /**
     * 字典信息缓存
     *
     * @return Object
     */
    @Override
    public Object cache() {
        // 判断缓存是否存在
        if (redisUtil.hasKey(Constant.DICT_CACHE)) {
            return JSONObject.parse(redisUtil.get(Constant.DICT_CACHE).toString());
        }
        // 重新查询组装
        Map<String, Map<String, SysDict>> result = new HashMap<>();
        List<SysDict> list = this.list();
        Map<String, List<SysDict>> collect = list.stream().collect(Collectors.groupingBy(SysDict::getDictTypeCode));
        collect.forEach((k, v) -> {
            Map<String, SysDict> map = v.stream()
                    .collect(Collectors.toMap(SysDict::getDictCode, dict -> dict));
            result.put(k, map);
        });
        // 设置缓存
        String str = JSON.toJSONString(result);
        redisUtil.set(Constant.DICT_CACHE, str);
        // 返回json
        return JSONObject.parse(str);
    }

    /**
     * 字典信息详情
     *
     * @param dictId 字典ID
     * @return SysDictVO
     */
    @Override
    public SysDictVO detailById(Long dictId) {
        return CopyUtils.classCopy(baseMapper.selectById(dictId), SysDictVO.class);
    }

    /**
     * 新增字典信息
     *
     * @param dto 字典dto
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean add(SysDictDTO dto) {
        // 删除字典缓存
        redisUtil.del(Constant.DICT_CACHE);
        // 判断公告信息是否存在
        List<SysDict> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysDict::getDictTypeCode, dto.getDictTypeCode())
                .eq(SysDict::getDictCode, dto.getDictCode())
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("字典信息已存在,新增失败!");
        }
        // 复制字典信息实体
        SysDict dict = CopyUtils.classCopy(dto, SysDict.class);
        // 保存字典信息，并返回结果
        return baseMapper.insert(dict) > 0;
    }

    /**
     * 编辑字典信息
     *
     * @param dictId 字典ID
     * @param dto    字典dto
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean edit(Long dictId, SysDictDTO dto) {
        // 删除字典缓存
        redisUtil.del(Constant.DICT_CACHE);
        // 判断公告信息是否存在
        List<SysDict> list = new LambdaQueryChainWrapper<>(baseMapper)
                .eq(SysDict::getDictTypeCode, dto.getDictTypeCode())
                .eq(SysDict::getDictCode, dto.getDictCode())
                .ne(SysDict::getDictId, dictId)
                .list();
        if (!CollectionUtils.isEmpty(list)) {
            throw new ServiceException("字典信息已存在,编辑失败!");
        }
        // 复制字典信息实体
        SysDict dict = CopyUtils.classCopy(dto, SysDict.class);
        // 设置字典ID
        dict.setDictId(dictId);
        // 编辑字典信息，并返回结果
        return baseMapper.updateById(dict) > 0;
    }

    /**
     * 删除字典信息
     *
     * @param dictId 字典ID
     * @return Boolean
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean del(Long dictId) {
        // 删除字典缓存
        redisUtil.del(Constant.DICT_CACHE);
        // 删除字典数据
        return baseMapper.deleteById(dictId) > 0;
    }
}
