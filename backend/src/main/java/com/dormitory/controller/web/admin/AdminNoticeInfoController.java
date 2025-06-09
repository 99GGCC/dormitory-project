package com.dormitory.controller.web.admin;


import com.baomidou.mybatisplus.core.metadata.IPage;
import com.dormitory.common.R;
import com.dormitory.controller.dto.NoticeInfoDTO;
import com.dormitory.controller.qry.NoticeInfoQry;
import com.dormitory.controller.vo.NoticeInfoVO;
import com.dormitory.service.NoticeInfoService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

/**
 * <p>
 * 公告信息表 前端控制器
 * </p>
 *
 * @author XXX
 * @since 2024-06-21
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/admin/notice/info")
@Api(value = "AdminNoticeInfoController", tags = {"公告信息接口"})
public class AdminNoticeInfoController {
    /**
     * 公告信息Service
     */
    private final NoticeInfoService noticeInfoService;

    @ApiOperation("公告信息分页查询")
    @GetMapping("/page")
    public R<IPage<NoticeInfoVO>> pageByQry(NoticeInfoQry qry) {
        return R.success(noticeInfoService.pageByQry(qry));
    }

    @ApiOperation("公告信息详情")
    @GetMapping("/detail/{noticeId}")
    public R<NoticeInfoVO> detail(@PathVariable @NotNull(message = "公告ID") Long noticeId) {
        return R.success(noticeInfoService.detailById(noticeId));
    }

    @ApiOperation("新增公告信息")
    @PostMapping("/add")
    public R<Boolean> add(@RequestBody @Valid NoticeInfoDTO dto) {
        return R.success(noticeInfoService.add(dto));
    }

    @ApiOperation("编辑公告信息")
    @PostMapping("/edit/{noticeId}")
    public R<Boolean> edit(@NotNull(message = "公告ID") @PathVariable Long noticeId,
                           @RequestBody @Valid NoticeInfoDTO dto) {
        return R.success(noticeInfoService.edit(noticeId, dto));
    }

    @ApiOperation("删除公告信息")
    @PostMapping("/del/{noticeId}")
    public R<Boolean> del(@NotNull(message = "公告ID") @PathVariable Long noticeId) {
        return R.success(noticeInfoService.del(noticeId));
    }
}

